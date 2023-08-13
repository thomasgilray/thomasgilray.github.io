

// Forgive this being so terrible, it was written up in about 20 minutes and never edited
// You may treat this as licenced under the MIT license.
// Otherwise, Copyright (c) Thomas Gilray, 2015


///////////////////////
// PARAMETERS /////////
///////////////////////

var size = 50;           // size of the blocks
var speed = 1.75;         // number of seconds per animation
var granularity = 35;    // timesteps per animation

///////////////////////






// Probably don't want to change these
var W = 0;
var H = 0;
var rawWpx = 1;
var rawHpx = 1;
var Wpx = 0;
var Hpx = 0;
var world = [];
var state = [];
var tick = [];
var steps = 0;



// Start on load
var oldsetup = function(){};
if (window.onload)
	oldsetup = window.onload;
window.onload = function() { checkAndSetup(); simulateConway(); oldsetup(); };



function checkAndSetup()
{
	// Check screen size
	var e = document.documentElement,
    	g = document.getElementsByTagName('body')[0],
    	x = window.innerWidth || e.clientWidth || g.clientWidth,
    	y = window.innerHeight|| e.clientHeight|| g.clientHeight;
		
	if (x == rawWpx && y == rawHpx)
	{
		// Already setup properly
		setTimeout(checkAndSetup, 700);
	}
	else
	{
		// Not setup yet or has been resized
		steps = 0;
		rawWpx = x;
		rawHpx = y;
		Wpx = (x + 4*size);
		Wpx -= Wpx % size;
		W = Wpx / size;
		Wpx += size/2;
		Hpx = (y + 4*size);
		Hpx -= Hpx % size;
		H = Hpx / size;
		Hpx += size/2;
		
		var str = "";
		for (var y = 0; y < H; ++y)
		{
			for (var x = 0; x < W; ++x)
			{
				if (Math.random() > 0.20)
					str += '<div class="box" id="box'+x+'_'+y+'"><div class="a"></div><div class="b"></div>'
							+ '<div class="c"></div><div class="d"></div><div class="e"></div></div>';
				else if (Math.random() > 0.40)
					str += '<div class="greenbox" id="box'+x+'_'+y+'"><div class="a"></div><div class="b"></div>'
							+ '<div class="c"></div><div class="d"></div><div class="e"></div></div>';
				else
					str += '<div class="lightgreenbox" id="box'+x+'_'+y+'"><div class="a"></div><div class="b"></div>'
							+ '<div class="c"></div><div class="d"></div><div class="e"></div></div>';
					
			}
			str += '<div class="break"></div>';
		}
		var conway = document.getElementById('conway');
		conway.style.width = Wpx + 'px';
		conway.style.height = Hpx + 'px';
		conway.style.top = '-' + Math.round(0.50*size) + 'px';
		conway.style.left = '-' + Math.round(0.80*size) + 'px';
		conway.innerHTML = str;
		
		world = [];
		for (var y = 0; y < H; ++y)
		{
			for (var x = 0; x < W; ++x)
			{
				world[x + y*W] = document.getElementById('box'+x+'_'+y);
				world[x + y*W].style.width = (size - 3) + 'px';
				world[x + y*W].style.height = (size - 3) + 'px';
				
				if (Math.random() < 0.2)
				{
					state[x + y*W] = true;
					world[x + y*W].style.opacity = 1.0;
				}
				else
				{
					state[x + y*W] = false;
					world[x + y*W].style.opacity = 0.0;
				}
			}
		}
		tick = state.slice();
		
		
		setTimeout(checkAndSetup, 300);
	}
}



function simulateConway()
{
	if (steps % (granularity*8) == 1)
	{
		// One step after each 8 animations, randomize just off the edge of the screen
		for (var y = 0; y < H; ++y)
		{
			for (var x = 0; x < W; ++x)
			{
				if (x > W - 4 || y > H - 4)
				{
					if (Math.random() > 0.5)
						tick[x + y*W] = true;
					else
						tick[x + y*W] = false;
				}
			}
		}
	}
	
	if (steps % granularity == 0)
	{
		// Set opacity for having reached tick
		for (var y = 0; y < H; ++y)
		{
			for (var x = 0; x < W; ++x)
			{
				if (tick[x + y*W])
				{
					world[x + y*W].style.opacity = 1.0;
				}
				else
				{
					world[x + y*W].style.opacity = 0.0;
				}
			}
		}
		
		var oldtick = tick.slice();
				
		// Produce tick, the next state
		for (var y = 0; y < H; ++y)
		{
			for (var x = 0; x < W; ++x)
			{
				var neighbors = 0;
				for (var i = -1; i <= 1; ++i)
				{
					for (var j = -1; j <= 1; ++j)
					{
						if ((i != 0 || j != 0) && oldtick[((x+i+W) % W) + ((y+j+H) % H)*W])
							++neighbors;
					}
				}
				
				if (neighbors == 3)
					tick[x + y*W] = true;
				else if (neighbors == 2)
					tick[x + y*W] = oldtick[x + y*W];
				else
					tick[x + y*W] = false;
			}
		}
		
		state = oldtick;
	
		++steps;
		setTimeout(simulateConway, Math.round(1600*speed*0.25));
	}
	else
	{
		// Set opacity based on linear interpolation
		for (var y = 0; y < H; ++y)
		{
			for (var x = 0; x < W; ++x)
			{
				if (tick[x + y*W] != state[x + y*W])
				{
					if (tick[x + y*W])
					{
						world[x + y*W].style.opacity = (steps % granularity) / (granularity * 1.0);
					}
					else
					{
						world[x + y*W].style.opacity = 1.0 - (steps % granularity) / (granularity * 1.0);
					}
				}
			}
		}

	
		++steps;
		var wait = Math.round(1000 * (0.60*speed) / (0.85*(granularity-1)));
		if (((steps % granularity)+1) / (granularity * 1.0) > 0.85)
			wait = Math.round(1000 * (0.15*speed) / (0.15*(granularity-1) * 1.0));
		setTimeout(simulateConway, wait);
	}
}


// A fun top-speed mode
function crazyConway()
{
	speed = 0.35;
	granularity = 5;
}





